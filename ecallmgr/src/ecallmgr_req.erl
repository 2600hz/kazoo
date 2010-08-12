%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive requests off the queue for call initialization.
%%% Spin up a call handler and hand off control.
%%% @end
%%% Created : 10 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_req).

-behaviour(gen_server).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {channel, ticket, queue, tag, callids=0}).

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
    format_log(info, "ECALLMGR_REQ(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:targeted_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "ECALLMGR_REQ: Accessing Exchange ~p~n", [Exchange]),

    QueueDeclare = amqp_util:new_targeted_queue(Ticket, ["callmgr." | net_adm:localhost()]),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_targeted(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    format_log(info, "ECALLMGR_REQ Bound ~p~n", [Queue]),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:basic_consume(Ticket, Queue),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, BasicConsume, self()),

    {ok, #state{channel=Channel, ticket=Ticket, queue=Queue}}.

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
    format_log(error, "ECALLMGR_REQ(~p): Exit received from ~p for ~p~n", [self(), _Pid, Reason]),
    {noreply, State};
%%{stop, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    handle_amqp_msg(Props#'P_basic'.content_type, Payload),
    {noreply, State};
handle_info(_Info, State) ->
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
handle_amqp_msg(<<"application/json">>, Payload) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "ECALLMGR_REQ(~p): Recv JSON: ~p~n", [self(), Prop]),
    process_req(get_route(Prop), Prop);
handle_amqp_msg(_ContentType, _Payload) ->
    format_log(error, "ECALLMGR_REQ(~p): Unknown ContentType: ~p Payload: ~p~n", [self(), _ContentType, _Payload]).

process_req({<<"request">>, <<"resource">>, <<"channel">>, <<"setup">>}, Prop) ->
    %% create and park a channel
    ecallmgr_call_sup:start_call([{setup, Prop}]),
    ok;
process_req({<<"request">>, <<"resource">>, <<"channel">>, <<"setup_loopback">>}, Prop) ->
    %% loopback
    format_log(info, "ECALLMGR_REQ(~p): Setup Loopback~n", [self()]),
    ecallmgr_app:start_call([{setup_loopback, Prop}]),
    ok;
process_req(_Route, _Prop) ->
    format_log(error, "ECALLMGR_REQ(~p): Unhandled route ~p~n", [self(), _Route]).

get_route(Prop) ->
    {
      get_value(<<"action">>, Prop)
     ,get_value(<<"category">>, Prop)
     ,get_value(<<"type">>, Prop)
     ,get_value(<<"command">>, Prop)
    }.
