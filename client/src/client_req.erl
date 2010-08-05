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
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/0, req_resource/0, start_script/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PUB_EXCHANGE, <<"broadcast">>).
-define(SUB_EXCHANGE, <<"targeted">>).

-record(state, {channel, connection, ticket, queue, tag, callid}).

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

start_script(CallId, ReqId) ->
    gen_server:cast(?MODULE, {start_script, CallId, ReqId}).

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

    Exchange = amqp_util:targeted_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "CALLMGR_REQ: Accessing Exchange ~p~n", [Exchange]),

    QueueDeclare = amqp_util:new_targeted_queue(Ticket, lists:flatten(["erl_client.", new_request_id()])),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_targeted(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:basic_consume(Ticket, Queue),
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

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
    Msg = [{request_id, new_request_id()}
	   ,{resp_queue_id, Queue}
	   ,{app, erl_client}
	   ,{action, request}
	   ,{category, resource}
	   ,{type, channel}
	   ,{command, find}
	   ,{respond_type, direct}
	   ,{respond_to, Queue}
	  ],

    {BasicPublish, AmqpMsg} = amqp_util:broadcast_publish(Ticket
							  , list_to_binary(mochijson2:encode({struct, Msg}))
							  , <<"application/json">>),
    %% execute the publish command
    Res = amqp_channel:call(Channel, BasicPublish, AmqpMsg),

    format_log(info, "CLIENT_REQ: Sent on Channel ~p~nMsg: ~p~nTo: ~p~nRes: ~p~n", [Channel, AmqpMsg, BasicPublish, Res]),
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
handle_cast({start_script, CallId, ReqId}, State) ->
    send_cmd(State, CallId, ReqId, lists:concat(["api uuid_kill ", CallId])),
    {noreply, State};
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
handle_info({{#'basic.return'{}, #amqp_msg{props = _Properties}}}=_Returned, State) ->
    format_log(info, "CLIENT_REQ: Returned: ~p~n", [_Returned]),
    {noreply, State};
handle_info({_Ignore, #amqp_msg{props = Props, payload = Payload}}, State) ->
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
		     {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
		     format_log(info, "CLIENT_REQ recv: JSON: ~p~n", [Prop]),
		     process_msg(State, get_route(Prop), Prop);
		 _ContentType ->
		     format_log(info, "~p recieved unknown msg type: ~p~n", [self(), _ContentType]),
		     State
	     end,
    {noreply, State1};
handle_info(Info, State) ->
    format_log(info, "CLIENT_REQ: Recv Unknown Info: ~p~n", [Info]),
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
%% Get params into proplist, then call me
% process_msg(State, {Action, Category, Type, Command}, Proplist)
process_msg(State, {<<"response">>
		    ,<<"event">>
		    ,EvtName
		    ,<<"report">>}, Prop) ->
    format_log(info, "CLIENT_REQ: Evt Received: ~p~n", [EvtName]),
    client_stats:resp_evt(get_value(<<"callid">>, Prop), EvtName),
    State;
process_msg(State, {<<"response">>
		   ,<<"resource">>
		   ,<<"channel">>
		   ,<<"setup">>}, Prop) ->
    %% stat tracking - call originated
    ReqId = get_value(<<"request_id">>, Prop, []),
    client_stats:resp_callid(ReqId),

    CallId = get_value(<<"callid">>, Prop),
    consume_call_evts(State, CallId),
    start_script(CallId, ReqId),
    State#state{callid=CallId};
process_msg(State, {<<"response">>
		   ,<<"resource">>
		   ,<<"channel">>
		   ,<<"setup_loopback">>}, Prop) ->
    %% stat tracking - call originated
    ReqId = get_value(<<"request_id">>, Prop, []),
    CallId = get_value(<<"callid">>, Prop),

    client_stats:resp_callid(ReqId),
    client_stats:link_reqid_callid(ReqId, CallId),

    consume_call_evts(State, CallId),
    start_script(CallId, ReqId),
    State#state{callid=CallId};
process_msg(State, {
	      <<"response">>
		  ,<<"resource">>
		  ,<<"channel">>
		  ,<<"find">>
	     }, Prop) ->
    spin_up_calls(State, get_value(<<"available_servers">>, Prop, [])),
    State;
process_msg(State, Route, Prop) ->
    format_log(info, "CLIENT_REQ: Unhandled Route: ~p~nProp: ~p~n", [Route, Prop]),
    State.

spin_up_calls(_State, []) -> ok;
spin_up_calls(#state{channel=Channel, ticket=Ticket, queue=Queue}=State, [H|Hs]) ->
    Msg = [{request_id, new_request_id()}
	   ,{resp_queue_id, Queue}
	   ,{app, erl_client}
	   ,{action, request}
	   ,{category, resource}
	   ,{type, channel}
	   ,{command, setup_loopback}
	   ,{respond_type, direct}
	   ,{respond_to, Queue}
	   ,{call, {struct, [{from, ["erl_client_name", 444]}, {to, 555}, {via, 666}]}}
	   ,{tenant_id, 9876}],
    HQ = list_to_binary(["targeted." | H]),
    {BasicPublish, AmqpMsg} = amqp_util:targeted_publish(Ticket
							 ,HQ
							 ,list_to_binary(mochijson2:encode({struct, Msg}))
							 ,<<"application/json">>
							),

    %% execute the publish command
    Res = amqp_channel:call(Channel, BasicPublish, AmqpMsg),
    client_stats:req_callid(get_value(request_id, Msg)),

    format_log(info, "CLIENT_REQ: Spinup on Host ~p~nMsg: ~p~nTo: ~p~nRes: ~p~n"
	       ,[HQ, AmqpMsg, BasicPublish, Res]),
    spin_up_calls(State, Hs).

send_cmd(#state{channel=Channel, ticket=Ticket, queue=Queue}, CallId, ReqId, Cmd) ->
    format_log(info, "CLIENT_REQ: Send Cmd: ~p~n", [Cmd]),

    Msg = [{request_id, ReqId}
	   ,{resp_queue_id, Queue}
	   ,{app, erl_client}
	   ,{action, request}
	   ,{category, callcontrol}
	   ,{type, command}
	   ,{command, execute}
	   ,{execute, Cmd}
	   ,{respond_type, direct}
	   ,{respond_to, Queue}
	  ],

    {Pub, AmqpMsg} = amqp_util:callctl_publish(Ticket
					       ,CallId
					       ,list_to_binary(mochijson2:encode({struct, Msg}))
					       ,<<"application/json">>),
    %% execute the publish command
    _Res = amqp_channel:call(Channel, Pub, AmqpMsg),
    client_stats:req_cmd(CallId, Cmd),
    format_log(info, "CLIENT_REQ: Sent Cmd ~p at ~p: ~n", [get_value(execute, Msg), erlang:now()]),
    ok.

consume_call_evts(#state{channel=Channel, ticket=Ticket}, CallId) ->
    QueueDeclare = amqp_util:new_callevt_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_callevt(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Register a consumer to listen to the queue
    BC = amqp_util:callevt_consume(Ticket, CallId),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, BC, self()),
    format_log(info, "CLIENT_REQ: Consume callevt.~p events BC: ~p at ~p~n", [CallId, BC, erlang:now()]).

get_route(Prop) ->
    {get_value(<<"action">>, Prop)
     ,get_value(<<"category">>, Prop)
     ,get_value(<<"type">>, Prop)
     ,get_value(<<"command">>, Prop)
    }.

%% borrowed from couchbeam_util, which borrowed from couch
new_request_id() ->
    to_hex(crypto:rand_bytes(16)).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
