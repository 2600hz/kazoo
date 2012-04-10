%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% Created : 23 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).

-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([process_route_req/4]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include_lib("ecallmgr/src/ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    process_flag(trap_exit, true),
    lager:debug("starting new fs route listener for ~s", [Node]),
    case freeswitch:bind(Node, dialplan) of
        ok ->
            lager:debug("bound to dialplan request on ~s", [Node]),
            {ok, #state{node=Node, options=Options}};
        {error, Reason} ->
            lager:warning("failed to bind to dialplan requests on ~s, ~p", [Node, Reason]),
            {stop, Reason};
        timeout ->
            lager:warning("timeout when trying to bind to dialplan requests on node ~s", [Node]),
            {stop, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    lager:debug("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p ID: ~s", [Node, _Section, _Something, _Key, _Value, ID]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    {noreply, State};
handle_info({fetch, dialplan, _Tag, _Key, _Value, FSID, [CallID | FSData]}, #state{node=Node}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData), props:get_value(<<"Caller-Context">>, FSData)} of
        {<<"REQUEST_PARAMS">>, ?WHISTLE_CONTEXT} ->
            %% TODO: move this to a supervisor somewhere
            process_route_req(Node, FSID, CallID, FSData),
            {noreply, State, hibernate};
        {_Other, _Context} ->
            lager:debug("ignoring event ~s in context ~s from ~s", [_Other, _Context, Node]),
            _ = freeswitch:fetch_reply(Node, FSID, ?EMPTYRESPONSE),
            {noreply, State, hibernate}
    end;
handle_info(_Other, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs route ~s termination: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec process_route_req/4 :: (atom(), ne_binary(), ne_binary(), proplist()) -> 'ok'.
process_route_req(Node, FSID, CallID, FSData) ->
    spawn(fun() ->
                  lager:debug("processing fetch request ~s (call ~s) from ~s", [FSID, CallID, Node]),
                  DefProp = [{<<"Msg-ID">>, FSID}
                             ,{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, FSData, 
                                                                     props:get_value(<<"Caller-Caller-ID-Name">>, FSData, <<"Unknown">>))}
                             ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, FSData, 
                                                                       props:get_value(<<"Caller-Caller-ID-Number">>, FSData, <<"0000000000">>))}
                             ,{<<"To">>, ecallmgr_util:get_sip_to(FSData)}
                             ,{<<"From">>, ecallmgr_util:get_sip_from(FSData)}
                             ,{<<"Request">>, ecallmgr_util:get_sip_request(FSData)}
                             ,{<<"Call-ID">>, CallID}
                             ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(FSData))}
                             | wh_api:default_headers(<<>>, <<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)],
                  %% Server-ID will be over-written by the pool worker
                  {ok, AuthZEnabled} = ecallmgr_util:get_setting(authz_enabled, true),
                  case wh_util:is_true(AuthZEnabled) of
                      true -> authorize_and_route(Node, FSID, CallID, FSData, DefProp);
                      false -> route(Node, FSID, CallID, DefProp, undefined)
                  end
          end).

-spec authorize_and_route/5 :: (atom(), ne_binary(), ne_binary(), proplist(), proplist()) -> 'ok'.
authorize_and_route(Node, FSID, CallID, FSData, DefProp) ->
    lager:debug("starting authorization request from node ~s", [Node]),
    {ok, AuthZPid} = ecallmgr_authz:authorize(FSID, CallID, FSData),
    route(Node, FSID, CallID, DefProp, AuthZPid).
    
-spec route/5 :: (atom(), ne_binary(), ne_binary(), proplist(), pid() | 'undefined') -> 'ok'.
route(Node, FSID, CallID, DefProp, AuthZPid) ->
    lager:debug("starting route request from node ~s", [Node]),
    case ecallmgr_amqp_pool:route_req(DefProp) of
        {ok, RespJObj} ->
            RouteCCV = wh_json:get_value(<<"Custom-Channel-Vars">>, RespJObj, wh_json:new()),
            authorize(Node, FSID, CallID, RespJObj, AuthZPid, RouteCCV);
        _Else ->
            lager:debug("did not receive route response: ~p", [_Else])
    end.

-spec authorize/6 :: (atom(), ne_binary(), ne_binary(), wh_json:json_object(), pid() | 'undefined', wh_json:json_object()) -> 'ok'.
authorize(Node, FSID, CallID, RespJObj, undefined, RouteCCV) ->
    lager:debug("no authz available, validating route_resp on node ~s", [Node]),
    true = wapi_route:resp_v(RespJObj),
    reply(Node, FSID, CallID, RespJObj, RouteCCV);
authorize(Node, FSID, CallID, RespJObj, AuthZPid, RouteCCV) ->
    lager:debug("checking authz_resp on node ~s", [Node]),
    case ecallmgr_authz:is_authorized(AuthZPid) of
        {false, _} ->
            lager:debug("sending reply to node ~s: authz is false", [Node]),
            reply_forbidden(Node, FSID);
        {true, CCVJObj} ->
            CCV = wh_json:to_proplist(CCVJObj),
            lager:debug("sending reply to node ~s: authz is true", [Node]),
            true = wapi_route:resp_v(RespJObj),
            lager:debug("sending reply to node ~s: valid route resp", [Node]),
            RouteCCV1 = lists:foldl(fun({K,V}, RouteCCV0) -> wh_json:set_value(K, V, RouteCCV0) end, RouteCCV, CCV),

            reply(Node, FSID, CallID, RespJObj, RouteCCV1, AuthZPid)
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden/2 :: (atom(), ne_binary()) -> 'ok'.
reply_forbidden(Node, FSID) ->
    {ok, XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"error">>}
                                                ,{<<"Route-Error-Code">>, <<"402">>}
                                                ,{<<"Route-Error-Message">>, <<"Payment Required">>}
                                               ]),
    case freeswitch:fetch_reply(Node, FSID, XML) of
        ok ->
            %% only start control if freeswitch recv'd reply
            lager:debug("node ~s accepted our route unauthz", [Node]);
        {error, Reason} ->
            lager:debug("node ~s rejected our route unauthz, ~p", [Node, Reason]);
        timeout ->
            lager:debug("received no reply from node ~s, timeout", [Node])
    end.

-spec reply/5 :: (atom(), ne_binary(), ne_binary(), wh_json:json_object(), wh_json:json_object()) -> 'ok'.
-spec reply/6 :: (atom(), ne_binary(), ne_binary(), wh_json:json_object(), wh_json:json_object(), pid() | 'undefined') -> 'ok'.
reply(Node, FSID, CallID, RespJObj, CCVs) ->
    reply(Node, FSID, CallID, RespJObj, CCVs, undefined).

reply(Node, FSID, CallID, RespJObj, CCVs, AuthZPid) ->
    {ok, XML} = ecallmgr_fs_xml:route_resp_xml(RespJObj),
    ServerQ = wh_json:get_value(<<"Server-ID">>, RespJObj),

    case freeswitch:fetch_reply(Node, FSID, XML) of
        ok ->
            %% only start control if freeswitch recv'd reply
            lager:debug("node ~s accepted our route (authzed), starting control and events", [Node]),
            start_control_and_events(Node, CallID, ServerQ, CCVs),
            ecallmgr_authz:authz_win(AuthZPid);
        {error, Reason} ->
            lager:debug("node ~s rejected our route response, ~p", [Node, Reason]);
        timeout ->
            lager:debug("received no reply from node ~s, timeout", [Node])
    end.

-spec start_control_and_events/4 :: (atom(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
start_control_and_events(Node, CallID, SendTo, CCVs) ->
    try
        {ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, CallID, SendTo),
        {ok, _EvtPid} = ecallmgr_call_sup:start_event_process(Node, CallID),

        CtlQ = ecallmgr_call_control:queue_name(CtlPid),

        CtlProp = [{<<"Msg-ID">>, CallID}
                   ,{<<"Call-ID">>, CallID}
                   ,{<<"Control-Queue">>, CtlQ}
                   ,{<<"Custom-Channel-Vars">>, CCVs}
                   | wh_api:default_headers(CtlQ, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)],
        send_control_queue(SendTo, CtlProp)
    catch
        _:Reason ->
            lager:debug("error during control handoff to whapp, ~p", [Reason]),
            {error, amqp_error}
    end.

-spec send_control_queue/2 :: (ne_binary(), proplist()) -> 'ok'.
send_control_queue(SendTo, CtlProp) ->
    lager:debug("sending route_win to ~s", [SendTo]),
    wapi_route:publish_win(SendTo, CtlProp).
