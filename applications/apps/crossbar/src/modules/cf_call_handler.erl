%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow call handler, waits for winning routes to spawn callflow processes
%%%
%%% @end
%%% Created :  3 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_call_handler ).

-behaviour ( gen_server ).

%% API
-export ( [get_q/1] ).

%% API
-export ( [start_link/3] ).

%% gen_server callbacks
-export ( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-import ( logger, [format_log/3] ).

-include ( "../crossbar.hrl" ).
-include_lib ( "webmachine/include/webmachine.hrl" ).
-include ( "../../../../utils/src/whistle_amqp.hrl" ).
-include ( "../../include/amqp_client/include/amqp_client.hrl" ).

-define ( SERVER, ?MODULE ).

-record ( state, {
   flow      = [] :: proplist(),
   amqp_h    = "" :: string(),
   amqp_q    = <<>> :: binary(),
   ctrl_q    = <<>> :: binary(),
   call_id   = <<>> :: binary(),
   req_prop  = [] :: proplist()
} ).


%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @public
% @doc
% Gets callmanager queue
%
% @end
%------------------------------------------------------------------------------
-spec ( get_q/1 :: ( Pid :: binary() ) -> binary() ).
get_q ( Pid ) -> {ok, Q} = gen_server:call(Pid, get_q), Q.


%------------------------------------------------------------------------------
% @public
% @doc
% Starts the server
%
% @end
%------------------------------------------------------------------------------
-spec ( start_link/3 :: ( AHost :: string(), ReqProp :: proplist(), Flow :: proplist ) ->
   tuple(ok, Pid :: pid()) | ignore | tuple(error, Error :: term()) )
.
start_link ( AHost, ReqProp, Flow ) -> gen_server:start_link( ?MODULE, [AHost, ReqProp, Flow], [] ).



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
-spec ( init/1 :: (Args :: list()) ->
     tuple(ok, #state{})
   | tuple(ok, #state{}, Timeout :: integer())
   | ignore
   | tuple(stop, Reason :: term())
).
init ( [AHost, ReqProp, Flow] ) ->
   CallId = proplists:get_value(<<"Call-ID">>, ReqProp),
   { 
      ok,
      #state{
         flow = Flow,
         amqp_h = AHost,
         amqp_q = get_amqp_queue(AHost, CallId),
         call_id = CallId,
         req_prop = ReqProp
      }, 
      0 
   }
.

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
handle_call ( get_q, _, #state{amqp_q=Q}=State ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): getting call queue...~n",
      [self()]
   ),
   { reply, {ok, Q}, State };
handle_call ( Request, From, State ) ->
   format_log(
      error,
      "CF CALL HANDLER (~p): Unhandled call message:~nRequest: ~p~nFrom: ~p~n",
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
      "CF CALL HANDLER (~p): Unhandled cast message:~nMessage: ~p~n",
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
handle_info ( #'basic.consume_ok'{}, State ) -> 
   format_log(info, "CF CALL HANDLER (~p): basic consume ok...~n", [self()]),
   { noreply, State };
handle_info ( {_, #amqp_msg{props=Proplist, payload=Payload}}, #state{flow=Flow}=State ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): handling request request...~nProplist: ~p~nPayload:~p~n",
      [self(), Proplist, Payload]
   ),
   {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
   case proplists:get_value(<<"Event-Name">>, Prop) of
      <<"route_win">> -> spawn(fun() -> callflow(Prop, Flow) end);
      _               -> format_log(info, "CF CALL HANDLER (~p): Ignoring message...~n~p~n", [self(), Prop])
   end,
   { noreply, State };
handle_info ( Info, State ) ->
   format_log(
      error,
      "CF CALL HANDLER (~p): Unhandled info message:~nInfo: ~p~n",
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
-spec ( get_amqp_queue/2 :: (AHost :: string(), CallId :: binary()) -> tuple(ok, binary()) ).
get_amqp_queue ( AHost, CallId ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): Getting AMQP queue...: ~p: ~p~n",
      [self(), AHost, CallId]
   ),

   AmqpQ = amqp_util:new_callevt_queue(AHost, <<>>),

   amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId, events),
   amqp_util:bind_q_to_targeted(AHost, AmqpQ),

   amqp_util:basic_consume(AHost, AmqpQ),

   format_log(
      info,
      "CF CALL HANDLER (~p): Consuming on call event queue: ~p~n",
      [self(), AmqpQ]
   ),
   AmqpQ
.



%%-----------------------------------------------------------------------------
%% CALLFLOW PROCESS
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% 
%%
%% @end
%%-----------------------------------------------------------------------------
callflow ( Proplist, Flow ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): Call is parked and callflow process is spawned~n",
      [self()]
   ),
   
   % sending the tree to execute
   execute(Flow)
.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Executes the head node of the tree and proceeds to the next child
%%
%% @end
%%-----------------------------------------------------------------------------
execute ( [] ) ->
   format_log(
      error,
      "CF CALL HANDLER (~p): Empty flow...~n",
      [self()]
   );
execute ( Tree ) ->
   Module = proplists:get_value(<<"module">>, Tree),
   Data = proplists:get_value(<<"data">>, Tree),
   format_log(
      info,
      "CF CALL HANDLER (~p): Executing module '~p' with data:~n~p~n",
      [self(), Module, Data]
   )
   % Get response and call itself with the next child
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
