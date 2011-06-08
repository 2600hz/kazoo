%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow route handler, listens to route requests and responds to
%%% inbound calls with valid callflows
%%%
%%% @end
%%% Created :      3  Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 17 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
-module(cf_responder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-record(state, {
	   amqp_q = <<>> :: binary()
         }).

%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------
%%

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
%%

%------------------------------------------------------------------------------
% @private
% @doc
% Initializes the server
%
% @end
%------------------------------------------------------------------------------
init([]) -> 
    ?LOG_SYS("starting new callflow responder"),
    _ = amqp_util:callmgr_exchange(),
    _ = amqp_util:targeted_exchange(),
    _ = amqp_util:callevt_exchange(),
    ?LOG_SYS("ensuring database ~s exists", [?CALLFLOW_DB]),
    couch_mgr:db_create(?CALLFLOW_DB),
    ?LOG_SYS("ensuring database ~s has view ~s", [?CALLFLOW_DB, ?VIEW_FILE]),
    try                              
        {ok, _} = couch_mgr:update_doc_from_file(?CALLFLOW_DB, callflow, ?VIEW_FILE)
    catch
        _:_ -> 
            couch_mgr:load_doc_from_file(?CALLFLOW_DB, callflow, ?VIEW_FILE)
    end,
    {ok, #state{}, 0}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
handle_call({find_flow, To}, From, State) ->
    spawn(fun() ->                  
                  case hunt_to(To) orelse hunt_no_match(To) of
                      true ->
                          {ok, {_, Flow, _}} = wh_cache:fetch({cf_flow, To}),
                          gen_server:reply(From, {ok, Flow});
                      false ->
                          gen_server:reply(From, {error, not_found})
                  end
          end),
    {noreply, State};
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
            ?LOG_SYS("attempting to connect amqp again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
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
                    ?LOG_SYS("attempting to reconnect amqp again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect amqp again in ~b ms", [Timeout]),
                    timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    Parent = self(),
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  handle_req(wh_json:get_value(<<"Event-Name">>, JObj), JObj, Parent, State)
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
    ?LOG_SYS("callflow responder ~p termination", [_Reason]),
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

%%-----------------------------------------------------------------------------
%% INTERNAL API
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% STARTING AMQP
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Creates and binds a queue to call manager route requests
%%
%% @end
%%-----------------------------------------------------------------------------
-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    try
        Q = amqp_util:new_callmgr_queue(<<>>),
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

%%-----------------------------------------------------------------------------
%% HANDLING ROUTE REQUESTS
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to handle route request
%%
%% @end
%%-----------------------------------------------------------------------------
-spec(handle_req/4 :: (EventName :: binary()|undefined, JObj:: json_object(), Parent :: pid(), State :: #state{}) -> ok|tuple(error, atom())).
handle_req(<<"route_req">>, JObj, _, #state{amqp_q=Q}) ->
    <<"dialplan">> = wh_json:get_value(<<"Event-Category">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId), %% we were spawn'd so this is safe
    ?LOG_START("received route request"),
    Dest = destination_uri(JObj),
    ?LOG("hunting for ~s", [Dest]),
    case hunt_to(Dest) orelse hunt_no_match(Dest) of
        false ->
            ?LOG_END("no callflows satisfy request");
        true ->
            wh_cache:store({cf_call, CallId}, {Dest, JObj}, 5),
            Resp = [
                     {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Routes">>, []}
                    ,{<<"Method">>, <<"park">>}
                    | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
                   ],
            {ok, Payload} = whistle_api:route_resp(Resp),
            ?LOG_END("replying to route request"),
            amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload)
    end;

handle_req(<<"route_win">>, JObj, Parent, #state{amqp_q=Q}) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId), %% we were spawn'd so this is safe
    ?LOG_START("received route win"),
    {ok, {Dest, RouteReq}} = wh_cache:fetch({cf_call, CallId}),
    {ok, {FlowId, Flow, AccountDb}} = wh_cache:fetch({cf_flow, Dest}),
    From = wh_json:get_value(<<"From">>, RouteReq),
    ?LOG("from: ~s", [From]),
    To = wh_json:get_value(<<"To">>, RouteReq),
    ?LOG("to: ~s", [To]),
    [ToNumber, ToRealm] = binary:split(To, <<"@">>),
    [FromNumber, FromRealm] = binary:split(From, <<"@">>),
    [DestNumber, DestRealm] = binary:split(Dest, <<"@">>),
    Call = #cf_call {
       ctrl_q=wh_json:get_value(<<"Control-Queue">>, JObj)
      ,bdst_q=Q
      ,call_id=CallId
      ,cf_responder=Parent
      ,account_db=AccountDb
      ,authorizing_id=wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], RouteReq)
      ,flow_id=FlowId
      ,cid_name=wh_json:get_value(<<"Caller-ID-Name">>, RouteReq)
      ,cid_number=wh_json:get_value(<<"Caller-ID-Number">>, RouteReq)
      ,destination=Dest
      ,dest_number=DestNumber
      ,dest_realm=DestRealm
      ,from=From
      ,from_number=FromNumber
      ,from_realm=FromRealm
      ,to=To
      ,to_number=ToNumber
      ,to_realm=ToRealm
      ,channel_vars=wh_json:get_value(<<"Custom-Channel-Vars">>, RouteReq)
     },
    ?LOG("caller-id: \"~s\" <~s>", [Call#cf_call.cid_name, Call#cf_call.cid_number]),
    ?LOG("call authorized by ~s", [Call#cf_call.authorizing_id]),
    cf_call_command:set(undefined, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteReq), Call),
    supervisor:start_child(cf_exe_sup, [Call, Flow]);

handle_req(_EventName, _JObj, _Parent, _State) ->
    {error, invalid_event}.

-spec(hunt_to/1 :: (To :: binary()) -> boolean()).
hunt_to(To) ->
    case wh_cache:fetch({cf_flow, To}) of         
        {ok, {FlowId, _, _}} ->
            ?LOG("found callflow ~s in cache", [FlowId]),
            true;
        {error, _} ->
            lookup_flow(To)
    end.

-spec(hunt_no_match/1 :: (To :: binary()) -> boolean()).
hunt_no_match(To) ->    
    [_, ToRealm] = binary:split(To, <<"@">>),
    lookup_flow(To, <<"no_match@", ToRealm/binary>>).

-spec(lookup_flow/1 :: (To :: binary()) -> boolean()).
lookup_flow(To) ->
    lookup_flow(To, To).

-spec(lookup_flow/2 :: (To :: binary(), Key :: binary()) -> boolean()).
lookup_flow(To, Key) ->
    case couch_mgr:get_results(?CALLFLOW_DB, ?VIEW_BY_URI, [{<<"key">>, Key}]) of
        {ok, []} ->
            false;
        {ok, [JObj]} ->
            FlowId = wh_json:get_value(<<"id">>, JObj),
            wh_cache:store({cf_flow, To}, {
                             FlowId,
                             wh_json:get_value([<<"value">>, <<"flow">>], JObj),
                             wh_json:get_value([<<"value">>, <<"account_db">>], JObj)
                            }, 500),
            ?LOG("retrieved callflow ~s from db", [FlowId]),
            true;
        {error, E} ->
            ?LOG("error looking up callflow ~p", [E]),
            false
    end.    

-spec(destination_uri/1 :: (JObj :: json_object()) -> binary()).
destination_uri(JObj) ->    
    [ToNumber, ToRealm] = binary:split(wh_json:get_value(<<"To">>, JObj, <<"unknown@norealm">>), <<"@">>), 
    <<(wh_json:get_value(<<"Destination-Number">>, JObj, ToNumber))/binary, $@, (wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Realm">>], JObj, ToRealm))/binary>>.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
