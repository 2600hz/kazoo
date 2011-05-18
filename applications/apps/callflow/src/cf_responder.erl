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
handle_info(#'basic.consume_ok'{}, State) -> 
    {noreply, State};
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
    <<"dialplan">> = wh_json:get_value(<<"Event-Category">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Dest = destination_uri(JObj),
    true = hunt_to(Dest) orelse hunt_no_match(Dest),
    logger:format_log(info, "CF_RESP(~p): Affirmative routing response for ~p", [self(), Dest]),
    wh_cache:store({cf_call, CallId}, {Dest, JObj}, 5),
    Resp = [
             {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            | whistle_api:default_headers(CQ, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = whistle_api:route_resp(Resp),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload);

handle_req(<<"route_win">>, JObj, Parent, #state{callmgr_q=CQ}) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    {ok, {Dest, RouteReq}} = wh_cache:fetch({cf_call, CallId}),
    {ok, {FlowId, Flow, AccountDb}} = wh_cache:fetch({cf_flow, Dest}),
    To = wh_json:get_value(<<"To">>, RouteReq),
    From = wh_json:get_value(<<"From">>, RouteReq),
    [ToNumber, ToRealm] = binary:split(To, <<"@">>),
    [FromNumber, FromRealm] = binary:split(From, <<"@">>),
    [DestNumber, DestRealm] = binary:split(Dest, <<"@">>),
    Call = #cf_call {
       ctrl_q=wh_json:get_value(<<"Control-Queue">>, JObj)
      ,bdst_q=CQ
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
    cf_call_command:set(undefined, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteReq), Call),
    supervisor:start_child(cf_exe_sup, [Call, Flow]);

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
    lookup_flow(To, <<"no_match@", ToRealm/binary>>).

-spec(lookup_flow/1 :: (To :: binary()) -> boolean()).
lookup_flow(To) ->
    lookup_flow(To, To).

-spec(lookup_flow/2 :: (To :: binary(), Key :: binary()) -> boolean()).
lookup_flow(To, Key) ->
    case couch_mgr:get_results(?CALLFLOW_DB, ?VIEW_BY_URI, [{<<"key">>, Key}]) of
        {ok, []} ->
            format_log(info, "CF_RESP(~p): Could not find callflow for ~p to ~p", [self(), Key, To]),
            false;
        {ok, [JObj]} ->
            format_log(info, "CF_RESP(~p): Retreived callflow for ~p to ~p", [self(), Key, To]),
            wh_cache:store({cf_flow, To}, {
                             wh_json:get_value(<<"id">>, JObj),
                             wh_json:get_value([<<"value">>, <<"flow">>], JObj),
                             wh_json:get_value([<<"value">>, <<"account_db">>], JObj)
                            }, 500),
            true;
        {error, _}=E ->
            format_log(info, "CF_RESP(~p): Error ~p while retreiving callflow ~p to ~p", [self(), E, Key, To]),
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
