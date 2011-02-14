%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow route handler, listens to route requests and responds to
%%% inbound calls with valid callflows
%%%
%%% @end
%%% Created :  3 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_route_handler ).

-behaviour ( gen_server ).

%% API
-export ( [start_link/0] ).

%% gen_server callbacks
-export ( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-import ( logger, [format_log/3] ).

-include ( "../crossbar.hrl" ).
-include_lib ( "webmachine/include/webmachine.hrl" ).
-include ( "../../../../utils/src/whistle_amqp.hrl" ).
-include( "../../include/amqp_client/include/amqp_client.hrl" ).

-define ( SERVER, ?MODULE ).
-define ( APP_NAME, <<"callflow">> ).
-define ( APP_VERSION, <<"0.2">> ).

-record ( state, {amqp_host = "" :: string(), callmgr_q = <<>> :: binary()} ).
%-record ( cb_context, {} ).


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
-spec ( start_link/0 :: ( ) -> tuple(ok, Pid :: pid()) | ignore | tuple(error, Error :: term()) ).
start_link ( ) -> gen_server:start_link( {local, ?SERVER}, ?MODULE, [], [] ).



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
-spec ( init/1 :: (_) ->
     tuple(ok, #state{})
   | tuple(ok, #state{}, Timeout :: integer())
   | ignore
   | tuple(stop, Reason :: term())
).
init ( [] ) -> { ok, #state{}, 0 }.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_call/3 :: (Request :: term(), From :: term(), State :: term()) -> 
     tuple(reply, Reply :: term(), State :: term())
   | tuple(reply, Reply :: term(), State :: term(), Timeout :: integer())
   | tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer())
   | tuple(stop, Reason :: term(), State :: term())
   | tuple(stop, Reason :: term(), Reply :: term(), State :: term())
).
handle_call ( Request, From, State ) ->
   format_log(
      error,
      "CF ROUTE HANDLER (~p): Unhandled call message:~nRequest: ~p~nFrom: ~p~n",
      [self(), Request, From]
   ),
   { reply, ok, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_cast/2 :: (Msg :: term(), State :: term()) -> 
     tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer())
   | tuple(stop, Reason :: term(), State :: term())
).
handle_cast ( Msg, State ) ->
   format_log(
      error,
      "CF ROUTE HANDLER (~p): Unhandled cast message:~nMessage: ~p~n",
      [self(), Msg]
   ),
   { noreply, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_info/2 :: (Info :: term(), State :: term()) -> 
     tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer())
   | tuple(stop, Reason :: term(), State :: term())
).
handle_info ( timeout, State ) ->
   format_log(info, "CF ROUTE HANDLER (~p): timeout...~n", [self()]),
   AHost = whapps_controller:get_amqp_host(),
   {ok, CQ} = start_amqp(AHost),
   { noreply, State#state{amqp_host=AHost, callmgr_q=CQ} };
handle_info ( #'basic.consume_ok'{}, State ) -> 
   format_log(info, "CF ROUTE HANDLER (~p): basic consume ok...~n", [self()]),
   { noreply, State };
handle_info ( {_, #amqp_msg{props=Proplist, payload=Payload}}, #state{}=State ) ->
   format_log(
      info,
      "CF ROUTE HANDLER (~p): handling route request...~nProplist: ~p~nPayload:~p~n",
      [self(), Proplist, Payload]
   ),
   spawn(fun() -> handle_req(Proplist#'P_basic'.content_type, Payload, State) end),
   { noreply, State };
handle_info ( Info, State ) ->
   format_log(
      error,
      "CF ROUTE HANDLER (~p): Unhandled info message:~nInfo: ~p~n",
      [self(), Info]
   ),
   { noreply, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
-spec ( terminate/2 :: (Reason :: term(), State :: term()) -> none() ).
terminate ( _Reason, _State ) -> ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
-spec ( code_change/3 :: (OldVsn :: term(), State :: term(), Extra :: term()) ->
   tuple(ok, NewState :: term())
).
code_change ( _OldVsn, State, _Extra ) -> { ok, State }.



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
-spec ( start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary()) ).
start_amqp ( AHost ) ->
   format_log(
      info,
      "CF ROUTE HANDLER (~p): Starting AMPQ: ~p~n",
      [self(), AHost]
   ),

   amqp_util:callmgr_exchange(AHost),
   amqp_util:targeted_exchange(AHost),
   amqp_util:callevt_exchange(AHost),
   CallmgrQueue = amqp_util:new_callmgr_queue(AHost, <<>>),

   % Bind the queue to an exchange
   amqp_util:bind_q_to_callmgr(AHost, CallmgrQueue, ?KEY_ROUTE_REQ),
   %amqp_util:bind_q_to_targeted(AHost, CallmgrQueue, CallmgrQueue),

   % Register a consumer to listen to the queue
   amqp_util:basic_consume(AHost, CallmgrQueue),

   format_log(
      info,
      "CF ROUTE HANDLER (~p): Consuming on call manager queue: ~p~n",
      [self(), CallmgrQueue]
   ),
   { ok, CallmgrQueue }
.



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
-spec ( handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> none() ).
handle_req ( <<"application/json">>, Payload, State ) ->
   {struct, JSON} = mochijson2:decode(binary_to_list(Payload)),
   format_log(
      info,
      "CF ROUTE HANDLER (~p): Received JSON: ~p~n",
      [self(), JSON]
   ),
   process_req(
      {
         proplists:get_value(<<"Event-Category">>, JSON),
         proplists:get_value(<<"Event-Name">>, JSON),
         proplists:get_value(<<"To">>, JSON)
      }, 
      JSON,
      State
   );
handle_req ( ContentType, _Payload, _State ) ->
   format_log(
      error,
      "CF ROUTE HANDLER (~p): Unknown content type: ~p~n",
      [self(), ContentType]
   )
.

%------------------------------------------------------------------------------
% @private
% @doc
% Decides whether to respond to request based on validation
%
% @end
%------------------------------------------------------------------------------
-spec ( process_req/3 :: (Msg_type :: tuple(binary(), binary(), To :: string()), Proplist :: proplist(), State :: #state{}) -> no_return() ).
process_req ( {<<"dialplan">>, <<"route_req">>, To}, Proplist, State ) ->
   format_log(
      progress,
      "CF ROUTE HANDLER (~p): Processing route request to: ~p~n",
      [self(), To]
   ),
   case validate(To) of
      { success, Flow } ->
         format_log(info, "CF ROUTE HANDLER (~p): Required callflow exists! Responding...~n", [self()]),
         RespQ = proplists:get_value(<<"Server-ID">>, Proplist),
         respond(RespQ, State, Proplist, Flow);
      { fail, Msg }     ->
         format_log(error, "CF ROUTE HANDLER (~p): ~p~n", [self(), Msg]);
      Unknown           ->
         format_log(error, "CF ROUTE HANDLER (~p): Unknown validation response: ~p~n", [self(), Unknown])
   end;
process_req ( Msg_type, Proplist, _State ) ->
   format_log(
      error,
      "CF ROUTE HANDLER (~p): Unknown route request:~nType: ~p~nProplist: ~p~n",
      [self(), Msg_type, Proplist]
   )
.

%------------------------------------------------------------------------------
% @private
% @doc
% 
%
% @end
%------------------------------------------------------------------------------
-spec ( respond/4 :: (RespQ :: binary(), State :: tuple(), ReqProp :: proplist(), Flow :: proplist()) -> none() ).
respond ( RespQ, #state{amqp_host=AHost}, ReqProp, Flow ) ->
   {ok, Pid} = cf_call_sup:start_proc([whapps_controller:get_amqp_host(), ReqProp, Flow]),
   Prop = [
      {<<"Test">>, <<"Testing was done successfully">>},
      {<<"Msg-ID">>, proplists:get_value(<<"Msg-ID">>, ReqProp)},
      {<<"Routes">>, []},
      {<<"Method">>, <<"park">>}
      | whistle_api:default_headers(cf_call_handler:get_q(Pid), <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
   ],
   {ok, JSON} = whistle_api:route_resp(Prop),
   amqp_util:targeted_publish(AHost, RespQ, JSON, <<"application/json">>)
.



%%-----------------------------------------------------------------------------
%% VALIDATING CALL + SOME DB WORK...
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @private
% @doc
% Searches the database against the receipient number and returns success or
% fail
%
% @end
%------------------------------------------------------------------------------
-spec ( validate/1 :: (To :: string()) -> success | tuple(fail, Reason :: string()) ).
validate ( To ) ->
   case binary:split(To, <<"@">>) of
      [Number|_] -> 
         Context = crossbar_doc:load_view({"callflow", "flow"}, [{<<"key">>, Number}], #cb_context{db_name="callflow"}),
         case Context#cb_context.doc of
            [{struct, Doc}] ->
               {struct, Flow} = proplists:get_value(<<"value">>, Doc),
               { success, Flow };
            _               -> { fail, "Cannot find an appropriate callflow" }
         end;
      _          -> { fail, "Unexpected adress..." }
   end
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
