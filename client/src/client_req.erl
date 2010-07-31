%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Simulate a client making requests to Rabbit and listening for
%%% responses.
%%% @end
%%% Created : 30 Jul 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(client_req).

-include("../include/amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-import(client_logger, [log/2, format_log/3]).

%% API
-export([start_link/0, req_resource/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PUB_EXCHANGE, <<"broadcast">>).
-define(SUB_EXCHANGE, <<"targeted">>).

-record(state, {channel, connection, ticket, queue, tag}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

req_resource() ->
    gen_server:call(?MODULE, req_resource).

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
init([]) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    format_log(info, "RSCMGR_REQ: Channel open to MQ: ~p Ticket: ~p~n", [Channel, Ticket]),

    process_flag(trap_exit, true),

    %% create my queue for others to send me messages
    Exchange = #'exchange.declare'{
      ticket = Ticket,
      exchange = ?SUB_EXCHANGE,
      type = <<"direct">>,
      passive = false,
      durable = false,
      auto_delete=false,
      internal = false,
      nowait = false,
      arguments = []
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "RSCMGR_REQ: Accessing Exchange ~p~n", [Exchange]),

    Queue = list_to_binary([?SUB_EXCHANGE | ".erl_client"]),
    QueueDeclare = #'queue.declare'{
      ticket = Ticket,
      queue = Queue,
      passive = false,
      durable = false,
      exclusive = true,
      auto_delete = true,
      nowait = false,
      arguments = []
     },
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = #'queue.bind'{
      ticket = Ticket,
      queue = Queue,
      exchange = ?SUB_EXCHANGE,
      routing_key = Queue,
      nowait = false,
      arguments = []
     },
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Register a consumer to listen to the queue
    BasicConsume = #'basic.consume'{
      ticket = Ticket,
      queue = Queue,
      consumer_tag = Queue,
      no_local = false,
      no_ack = true,
      exclusive = true,
      nowait = false
     },
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

    %% If the registration was sucessful, then consumer will be notified
    receive
        #'basic.consume_ok'{consumer_tag = Tag} -> log(info, "CLIENT_REQ: receive ok");
	O -> format_log(info, "CLIENT_REQ: receive not ok ~p~n", [O])
    after
	1000 -> log(info, "CLIENT_REQ: timed out")
    end,

    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue}}.

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
handle_call(req_resource, _From, #state{channel=Channel, ticket=Ticket, queue=Queue}=State) ->
    BasicPublish = #'basic.publish'{
      ticket = Ticket
      ,exchange = ?PUB_EXCHANGE
      ,mandatory = false
      ,immediate = false
    },

    Msg = [{request_id, 1234}
	   ,{resp_queue_id, Queue} % (app.id)
	   ,{app, erl_client}
	   ,{action, request}
	   ,{category, resource}
	   ,{type, channel} % | message | media,
	   ,{command, find}
	   ,{respond_type, direct}
	   ,{respond_to, Queue}
	  ],

    %% Add the message to the publish, converting to binary
    AmqpMsg = #'amqp_msg'{payload = list_to_binary(mochijson2:encode({struct, Msg})),
			  props=#'P_basic'{content_type= <<"application/json">>}},

    %% execute the publish command
    Res = amqp_channel:call(Channel, BasicPublish, AmqpMsg),

    format_log(info, "Sent on Channel ~p~nMsg: ~p~nTo: ~p~nRes: ~p~n", [Channel, AmqpMsg, BasicPublish, Res]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    format_log(info, "CLIENT_REQ: Unknown Call - Req: ~p~n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

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
    format_log(info, "CLIENT_REQ: Unknown cast Msg: ~p~n", [_Msg]),
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
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
%% take in any incoming amqp messages and distribute
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    format_log(info, "CLIENT_REQ: Headers: ~p~nPayload: ~p~n", [Props, Payload]),
    State1 = case Props#'P_basic'.content_type of
		 <<"text/xml">> ->
		     log(info, xml),
		     %%notify(State#state.consumers, Props, xmerl_scan:string(binary_to_list(Payload)));
		     State;
		 <<"text/plain">> ->
		     log(info, text),
		     %%notify(State#state.consumers, Props, binary_to_list(Payload));
		     State;
		 <<"application/json">> ->
		     log(info, json),
		     {struct, Json} = mochijson2:decode(binary_to_list(Payload)),
		     format_log(info, "CLIENT_REQ: JSON: ~p~n", [Json]),
		     State;
		 _ContentType ->
		     format_log(info, "~p recieved unknown msg type: ~p~n", [self(), _ContentType]),
		     State
	     end,
    format_log(info, "Received ~p~n", [Payload]),
    {noreply, State1};
handle_info(Info, State) ->
    format_log(info, "CLIENT_REQ: Unknown Info: ~p~n", [Info]),
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
terminate(_Reason, _State) ->
    ok.

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
