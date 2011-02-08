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
-export ( [get_q/0] ).

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

-record ( state, {amqp_host = "" :: string(), amqp_q = <<>> :: binary()} ).


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
-spec ( get_q/0 :: ( ) -> binary() ).
get_q ( ) -> {ok, Q} = gen_server:call(?MODULE, get_q), Q.


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
handle_info ( timeout, State ) ->
   format_log(info, "CF CALL HANDLER (~p): timeout...~n", [self()]),
   AHost = whapps_controller:get_amqp_host(),
   {ok, Q} = start_amqp(AHost),
   { noreply, State#state{amqp_host=AHost, amqp_q=Q} };
handle_info ( #'basic.consume_ok'{}, State ) -> 
   format_log(info, "CF CALL HANDLER (~p): basic consume ok...~n", [self()]),
   { noreply, State };
handle_info ( {_, #amqp_msg{props=Proplist, payload=Payload}}, #state{}=State ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): handling request request...~nProplist: ~p~nPayload:~p~n",
      [self(), Proplist, Payload]
   ),
   {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
   case proplists:get_value(<<"Event-Name">>, Prop) of
      <<"route_win">> -> spawn(fun() -> callflow(Prop) end);
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
-spec ( start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary()) ).
start_amqp ( AHost ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): Starting AMPQ: ~p~n",
      [self(), AHost]
   ),

   AmqpQ = amqp_util:new_callevt_queue(AHost, <<>>),

   amqp_util:bind_q_to_callevt(AHost, AmqpQ, <<"">>, events),
   amqp_util:bind_q_to_targeted(AHost, AmqpQ),

   amqp_util:basic_consume(AHost, AmqpQ),

   format_log(
      info,
      "CF CALL HANDLER (~p): Consuming on call event queue: ~p~n",
      [self(), AmqpQ]
   ),
   { ok, AmqpQ }
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
callflow ( Proplist ) ->
   format_log(
      info,
      "CF CALL HANDLER (~p): Call is parked and callflow process is spawned~n",
      [self()]
   ),
   format_log(
      info,
      "CF CALL HANDLER (~p): Proplist with control queue:~n~p~n",
      [self(), Proplist]
   )
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
