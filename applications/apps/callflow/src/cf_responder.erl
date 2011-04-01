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
-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"0.2">> ).
-define(CACHE_SIZE, 100).
-define(MAINTANCE_CYCLE, 60000).

-define(CALLFLOW_DB, "callflow").
-define(VIEW_FILE, <<"views/callflows.json">>).
-define(VIEW_BY_URI, {?CALLFLOW_DB, <<"listing_by_uri">>}).

-record(state, {
           callmgr_q = <<>> :: binary()          
          ,flows = undefined :: undefined | dict()
          ,calls = undefined :: undefined | dict()
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
    gen_server:start_link(?MODULE, [], []).

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
    {ok, CQ} = start_amqp(),
    timer:send_after(?MAINTANCE_CYCLE, self(), {maintain_dicts}),
    {ok, #state{callmgr_q=CQ, flows=dict:new(), calls=dict:new()}}.

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
handle_cast({rm_flow, FlowId}, #state{flows=Flows, calls=Calls}=State) ->
    Flows1 = dict:filter(fun(_, {F, _, _}) when F == FlowId -> false;
                            (_, _) -> true
                         end, Flows),
    Calls1 = dict:filter(fun(_, {_, _, F, _}) when F == FlowId -> false;
                            (_, _) -> true
                         end, Calls),
    {noreply, State#state{flows=Flows1, calls=Calls1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
handle_info(#'basic.consume_ok'{}, State) -> 
    {noreply, State};
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    Parent = self(),
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  handle_req(whapps_json:get_value(<<"Event-Name">>, JObj), JObj, Parent, State)
          end),
    {noreply, State};
handle_info({add_flow, JObj, RouteReq}, #state{flows=Flows, calls=Calls}=State) ->
    FlowId = whapps_json:get_value(<<"id">>, JObj),
    Flow = whapps_json:get_value(<<"flow">>, JObj),
    CallId = whapps_json:get_value(<<"Call-ID">>, RouteReq),
    To = whapps_json:get_value(<<"To">>, RouteReq),
    TS = calendar:datetime_to_gregorian_seconds({date(), time()}),
    format_log(info, "CF_RESP(~p): Associate call flow ~p for ~p to call ~p", [self(), FlowId, To, CallId]),    
    {noreply, State#state{
                 flows=dict:store(To, {FlowId, Flow, TS}, Flows)
                ,calls=dict:store(CallId, {To, RouteReq, FlowId, TS}, Calls)
               }};
handle_info({maintain_dicts}, #state{flows=Flows, calls=Calls}=State) ->
    C1 = case dict:size(Calls) of
             0 ->
                 Calls;
             _ ->
                 E1 = calendar:datetime_to_gregorian_seconds({date(), time()}) - 5,
                 dict:filter(fun(_, {_, _, _, TS}) when TS < E1 -> false;
                                (_, _) -> true
                             end, Calls)
         end,
    F1 = case dict:size(Flows) of
             0 ->
                 Flows;
             _ ->
                 E2 = calendar:datetime_to_gregorian_seconds({date(), time()}) - 3600,
                 dict:filter(fun(_, {_, _, _, TS}) when TS < E2 -> false;
                                (_, _) -> true
                             end, Calls)
         end,
    timer:send_after(?MAINTANCE_CYCLE, self(), {maintain_dicts}),
    {noreply, State#state{calls=C1, flows=F1}};
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
   amqp_util:callmgr_exchange(),
   amqp_util:targeted_exchange(),
   amqp_util:callevt_exchange(),
   CallmgrQ = amqp_util:new_callmgr_queue(<<>>),
   amqp_util:bind_q_to_callmgr(CallmgrQ, ?KEY_ROUTE_REQ),
   amqp_util:bind_q_to_targeted(CallmgrQ, CallmgrQ),
   amqp_util:basic_consume(CallmgrQ),
   {ok, CallmgrQ}.

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
handle_req(<<"route_req">>, JObj, Parent, #state{callmgr_q=CQ, flows=Flows}) ->
    <<"dialplan">> = whapps_json:get_value(<<"Event-Category">>, JObj),
    {ok, Flow} = find_callflow(whapps_json:get_value(<<"To">>, JObj), Flows),
    Parent ! {add_flow, Flow, JObj},
    Resp = [
             {<<"Msg-ID">>, whapps_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            | whistle_api:default_headers(CQ, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = whistle_api:route_resp(Resp),
    amqp_util:targeted_publish(whapps_json:get_value(<<"Server-ID">>, JObj), Payload);
handle_req(<<"route_win">>, JObj, _Parent, #state{callmgr_q=CQ, flows=Flows, calls=Calls}) ->
    CallId = whapps_json:get_value(<<"Call-ID">>, JObj),
    {To, RouteReq, _, _} = dict:fetch(CallId, Calls),
    {_, Flow, _} = dict:fetch(To, Flows),
    [ToNumber, ToRealm] = binary:split(whapps_json:get_value(<<"To">>, RouteReq), <<"@">>),
    [FromNumber, FromRealm] = binary:split(whapps_json:get_value(<<"From">>, RouteReq), <<"@">>),
    Call = #cf_call {
       call_id = CallId
      ,bdst_q=CQ
      ,ctrl_q=whapps_json:get_value(<<"Control-Queue">>, JObj)
      ,to_number=ToNumber
      ,to_realm=ToRealm
      ,from_number=FromNumber
      ,from_realm=FromRealm
      ,route_request=RouteReq
     },
    format_log(info, "CF_RESP(~p): Executing callflow for ~p(~p)", [self(), To, CallId]),
    spawn(fun() -> cf_exe:start(Call, Flow) end);
handle_req(_EventName, _JObj, _Parent, _State) ->
    {error, invalid_event}.

-spec(find_callflow/2 :: (To :: binary()|undefined, Flows :: dict()|undefined) -> tuple(ok, json_object())|tuple(error, atom())).
find_callflow(To, Flows) ->
    case dict:find(To, Flows) of
        {ok, {I, F, _}} ->
            format_log(info, "CF_RESP(~p): Callflow for ~p exists in cache...", [self(), To]),
            {ok, {struct, [{<<"id">>, I}, {<<"flow">>, F}]}};
        error ->
            case couch_mgr:get_results(?CALLFLOW_DB, ?VIEW_BY_URI, [{<<"key">>, To}]) of
                {ok, []} ->
                    format_log(info, "CF_RESP(~p): Could not find callflow for ~p", [self(), To]),
                    {error, not_found};
                {ok, [JObj]} ->
                    format_log(info, "CF_RESP(~p): Retreived callflow for ~p", [self(), To]),
                    {ok, whapps_json:get_value(<<"value">>, JObj)};
                {error, _}=E ->
                    format_log(info, "CF_RESP(~p): Error ~p while retreiving callflow ~p", [self(), E, To]),
                    E
            end
    end.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
