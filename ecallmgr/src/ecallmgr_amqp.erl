%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Simple gen-server to to publish AMQP messages, for when passing
%%% around Channel and Ticket vars isn't feasible (e.g. call control)
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp).

-behaviour(gen_server).

%% API
-export([start_link/0, publish/3, publish_prop/3, delete_queue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE). 

-record(state, {channel, ticket}).

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

publish(Msg, Exchange, Queue) ->
    gen_server:cast(?MODULE, {publish, Msg, Exchange, Queue}).

%% encodes Msg as JSON and sends it to Exchange.Queue
publish_prop(Prop, Exchange, Queue) ->
    gen_server:cast(?MODULE, {publish_prop, Prop, Exchange, Queue}).

delete_queue(Q) ->
    gen_server:cast(?MODULE, {delete_queue, Q}).

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
    {ok, #state{channel=Channel, ticket=Ticket}}.

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
handle_cast({delete_queue, Q}, #state{channel=Channel, ticket=Ticket}=State) ->
    QD = amqp_util:queue_delete(Ticket, Q),
    amqp_channel:cast(Channel, QD),
    {noreply, State};
handle_cast({publish_prop, Prop, Exchange, Queue}, #state{channel=Channel, ticket=Ticket}=State) ->
    JSON = list_to_binary(mochijson2:encode({struct, Prop})),
    {BP, AmqpMsg} = publish(Ticket, Exchange, Queue, JSON),
    format_log(info, "ECALL_AMQP(~p): Pub Prop ~p~n", [self(), Prop]),
    amqp_channel:cast(Channel, BP, AmqpMsg),
    {noreply, State};
handle_cast({publish, Msg, Exchange, Queue}, #state{channel=Channel, ticket=Ticket}=State) ->
    {BP, AmqpMsg} = publish(Ticket, Exchange, Queue, Msg),
    format_log(info, "ECALL_AMQP(~p): Pub ~p~n", [self(), Msg]),
    amqp_channel:cast(Channel, BP, AmqpMsg),
    {noreply, State};
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
handle_info(_Info, State) ->
    format_log(info, "ECALL_AMQP(~p): Unhandled Info: ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{channel=Channel}) ->
    case erlang:is_process_alive(Channel) of
	true ->
	    amqp_manager:close_channel(Channel);
	false ->
	    ok
    end.

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

publish(Ticket, targeted, Queue, Msg) ->
    amqp_util:targeted_publish(Ticket, Queue, Msg, <<"application/json">>);
publish(Ticket, broadcast, _Queue, Msg) ->
    amqp_util:broadcast_publish(Ticket, Msg, <<"application/json">>);
publish(Ticket, callevt, Queue, Msg) ->
    amqp_util:callevt_publish(Ticket, Queue, Msg, <<"application/json">>);
publish(Ticket, callctl, Queue, Msg) ->
    amqp_util:callctl_publish(Ticket, Queue, Msg, <<"application/json">>);
publish(Ticket, resource, _Queue, Msg) ->
    amqp_util:resource_publish(Ticket, Msg, <<"application/json">>).
