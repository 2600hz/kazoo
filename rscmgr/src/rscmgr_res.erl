%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Consume resource report messages off the Resource Xc
%%% Update internal list of known resources
%%% Filter list based on resource requested and return sublist of
%%% matching resources.
%%% @end
%%% Created : 29 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(rscmgr_res).

-include("../include/amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, get_resource/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(rscmgr_logger, [log/2, format_log/3]).

-define(SERVER, ?MODULE).
-define(EXCHANGE, <<"resource">>).

-record(state, {known=[], channel, connection, ticket, queue, tag}).

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

get_resource(Type) ->
    gen_server:call(?MODULE, {get_resource, Type}).

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
    format_log(info, "RSCMGR_RES: Channel open to MQ: ~p Ticket: ~p~n", [Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = #'exchange.declare'{
      ticket = Ticket,
      exchange = ?EXCHANGE,
      type = <<"fanout">>,
      passive = false,
      durable = false,
      auto_delete=false,
      internal = false,
      nowait = false,
      arguments = []
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "RSCMGR_RES: Creating or accessing Exchange ~p~n", [Exchange]),

    Queue = list_to_binary(["resource." | net_adm:localhost()]),
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
        exchange = ?EXCHANGE,
        routing_key = Queue,
        nowait = false,
        arguments = []
    },
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    format_log(info, "RSCMGR_RES Bound ~p to ~p~n", [Queue, ?EXCHANGE]),

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
handle_call({get_resource, Type}, _From, #state{known=Known}=State) ->
    %% simple filtration for now; eventually support more ways to narrow list
    %% like geo-location and other criteria
    %% maybe a chain of filters that whittle the list down
    format_log(info, "Pre-filter of ~p Known: ~p~n", [Type, Known]),
    RightTypes = lists:filter(fun({_H, Props}) ->
				      SupportedTypes = proplists:get_value(resource_types, Props, []),
				      format_log(info, "SupTypes for ~p: ~p~n", [_H, SupportedTypes]),
				      lists:member(Type, SupportedTypes)
			      end, Known),
    {reply, proplists:get_keys(RightTypes), State};
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
%% cleanly exit if we have been asked to exit
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
%% take in any incoming amqp messages and distribute
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    State1 = case Props#'P_basic'.content_type of
		 <<"text/xml">> ->
		     log(info, xml),
		     %%notify(State#state.consumers, Props, xmerl_scan:string(binary_to_list(Payload)));
		     State;
		 <<"text/plain">> ->
		     log(info, text),
		     %%notify(State#state.consumers, Props, binary_to_list(Payload));
		     State;
		 <<"erlang/term">> ->
		     update_known_resources(State, binary_to_term(Payload));
		 undefined ->
		     try binary_to_term(Payload) of
			 Term -> update_known_resources(State, Term)
		     catch
			 _:_ -> State
		     end;
		 _ContentType -> format_log(info, "~p recieved unknown msg type: ~p~n", [self(), _ContentType]),
				 State
	     end,
    io:format("Known Resources: ~p~n", [State1#state.known]),
    {noreply, State1};
%% catch all so we dont loose state
handle_info(Unhandled, State) ->
    format_log(info, "~p unknown info request: ~p~n", [self(), Unhandled]),
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
update_known_resources(#state{known=Known}=State, Resource) ->
    %% todo - process resource to get weight
    Host = proplists:get_value(hostname, Resource),
    case proplists:get_value(Host, Known) of
	undefined ->
	    State#state{known=[{Host, Resource} | Known]};
	_Rsc ->
	    Known1 = proplists:delete(Host, Known),
	    State#state{known=[{Host, Resource} | Known1]}
    end.
