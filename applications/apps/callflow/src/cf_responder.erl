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
           callmgr_q = <<>> :: binary()          
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
    couch_mgr:db_create(?CALLFLOW_DB),
    couch_mgr:load_doc_from_file(?CALLFLOW_DB, callflow, ?VIEW_FILE),
    {ok, #state{callmgr_q=CQ}}.

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
handle_info(#'basic.consume_ok'{}, State) -> 
    {noreply, State};
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    Parent = self(),
%%    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  handle_req(whapps_json:get_value(<<"Event-Name">>, JObj), JObj, Parent, State),
%%          end),
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
handle_req(<<"route_req">>, JObj, _, #state{callmgr_q=CQ}) ->
    <<"dialplan">> = whapps_json:get_value(<<"Event-Category">>, JObj),
    CallId = whapps_json:get_value(<<"Call-ID">>, JObj),
    To = whapps_json:get_value(<<"To">>, JObj),
    From = whapps_json:get_value(<<"From">>, JObj),
    true = hunt_to(To) orelse hunt_no_match(To),
    wh_cache:store({cf_call, CallId}, {To, From, JObj}, 5),
    Resp = [
             {<<"Msg-ID">>, whapps_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            | whistle_api:default_headers(CQ, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = whistle_api:route_resp(Resp),
    amqp_util:targeted_publish(whapps_json:get_value(<<"Server-ID">>, JObj), Payload);

handle_req(<<"route_win">>, JObj, Parent, #state{callmgr_q=CQ}) ->
    CallId = whapps_json:get_value(<<"Call-ID">>, JObj),
    {ok, {To, From, RouteReq}} = wh_cache:fetch({cf_call, CallId}),
    {ok, {FlowId, Flow, AccountDb}} = wh_cache:fetch({cf_flow, To}),
    [ToNumber, ToRealm] = binary:split(To, <<"@">>),
    [FromNumber, FromRealm] = binary:split(From, <<"@">>),
    Call = #cf_call {
       call_id=CallId
      ,flow_id=FlowId
      ,cf_responder=Parent
      ,bdst_q=CQ
      ,ctrl_q=whapps_json:get_value(<<"Control-Queue">>, JObj)
      ,account_db=AccountDb
      ,to=To
      ,to_number=ToNumber
      ,to_realm=ToRealm
      ,from=From
      ,from_number=FromNumber
      ,from_realm=FromRealm
      ,route_request=RouteReq
     },
    format_log(info, "CF_RESP(~p): Executing callflow for ~p(~p)", [self(), To, CallId]),
    spawn(fun() -> cf_exe:start(Call, Flow) end);

handle_req(_EventName, _JObj, _Parent, _State) ->
    {error, invalid_event}.

-spec(hunt_to/1 :: (To :: binary()) -> boolean()).
hunt_to(To) ->
    case wh_cache:fetch({cf_flow, To}) of         
        {ok, _} ->
            format_log(info, "CF_RESP(~p): Callflow for ~p exists in cache...", [self(), To]),
            true;
        {error, _} ->
            lookup_flow(To)
    end.

-spec(hunt_no_match/1 :: (To :: binary()) -> boolean()).
hunt_no_match(To) ->    
    [_, ToRealm] = binary:split(To, <<"@">>),
    NoMatch = <<"no_match@", ToRealm/binary>>,
    case wh_cache:fetch({cf_flow, To}) of
        {ok, _} ->
            format_log(info, "CF_RESP(~p): Callflow for ~p exists in cache...", [self(), NoMatch]),
            true;
        {error, _} ->
            lookup_flow(NoMatch)
    end.    

-spec(lookup_flow/1 :: (To :: binary()) -> boolean()).
lookup_flow(To) ->
    case couch_mgr:get_results(?CALLFLOW_DB, ?VIEW_BY_URI, [{<<"key">>, To}]) of
        {ok, []} ->
            format_log(info, "CF_RESP(~p): Could not find callflow for ~p", [self(), To]),
            false;
        {ok, [JObj]} ->
            format_log(info, "CF_RESP(~p): Retreived callflow for ~p: ~p", [self(), To, JObj]),
            wh_cache:store({cf_flow, To}, {
                             whapps_json:get_value(<<"id">>, JObj),
                             whapps_json:get_value([<<"value">>, <<"flow">>], JObj),
                             whapps_json:get_value([<<"value">>, <<"account_db">>], JObj)
                            }, 500),
            true;
        {error, _}=E ->
            format_log(info, "CF_RESP(~p): Error ~p while retreiving callflow ~p", [self(), E, To]),
            false
    end.    
%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
