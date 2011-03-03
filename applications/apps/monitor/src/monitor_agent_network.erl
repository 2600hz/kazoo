%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for runnning the network monitoring tasks
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_agent_network).

-behaviour(gen_server).

-include("../include/monitor_amqp.hrl").

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/1]).
-export([set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {
         amqp_host  = false  :: string() | false
        ,agent_q    = false  :: binary() | false
    }).

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
start_link(AHost) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AHost], []).

set_amqp_host(AHost) ->
    gen_server:call(?SERVER, {set_amqp_host, AHost}, infinity).

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
init([AHost]) ->
    format_log(info, "MONITOR_AGENT_NETWORK(~p): Starting server with amqp host ~p~n", [self(), AHost]),
    {ok, Agent_Q} = start_amqp(AHost),
    {ok, #state{amqp_host=AHost, agent_q=Agent_Q}}.

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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=CurAHost} = State) ->
    format_log(info, "MONITOR_AGENT_NETWORK(~p): Updating amqp host from ~p to ~p~n", [self(), CurAHost, AHost]),
    amqp_manager:close_channel(self(), CurAHost),
    {ok, Agent_Q} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host = AHost, agent_q = Agent_Q}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

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
    format_log(error, "MONITOR_AGENT_CALL(~p): Received EXIT(~p) from ~p...~n", [self(), Reason, _Pid]),
    {stop, Reason, State};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    case amqp_util:is_json(Props) of
        true ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            spawn(fun() -> process_req(amqp_util:get_msg_type(Msg), Msg, State) end);
        _ ->
            format_log(info, "MONITOR_AGENT_NETWORK(~p): Recieved non JSON AMQP msg content type~n", [self()])
    end,
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
    format_log(error, "MONITOR_AGENT_NETWORK(~p): Going down(~p)...~n", [self(), _Reason]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures the monitor exchange exists, then creates a named queue
%% and places a consumer on it
%%
%% @spec(start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary())).
%% @end
%%--------------------------------------------------------------------

start_amqp(AHost) ->
    amqp_util:monitor_exchange(AHost),
    Agent_Q = amqp_util:new_monitor_queue(AHost, ?SERVER),
    %% Bind the queue to the topic exchange
    amqp_util:bind_q_to_monitor(AHost, Agent_Q, ?KEY_AGENT_NET_REQ),
    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(AHost, Agent_Q, [{exclusive, false}]),
    {ok, Agent_Q}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts destination from the proplist
%%
%% @spec(get_msg_destination/1 :: (Prop :: proplist()) -> list()).
%% @end
%%--------------------------------------------------------------------
get_msg_destination(Prop) ->
     Dest = get_value(<<"Destination">>, Prop, <<"localhost">>),
     binary_to_list(Dest).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the requests recieved from AMQP
%%
%% @spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), 
%%      Prop :: proplist(), State :: #state{}) -> no_return()).
%% @end
%%--------------------------------------------------------------------
process_req({<<"task">>, <<"ping_net_req">>}, Prop, #state{amqp_host = AHost, agent_q = Agent_Q}) ->
    case monitor_api:ping_net_req_v(Prop) of
        true ->
            Resp       = monitor_icmp:ping_test(get_msg_destination(Prop)),
            RespQ      = get_value(<<"Server-ID">>, Prop),
            Defaults   = monitor_util:prop_updates([{<<"Server-ID">>, Agent_Q}, {<<"Event-Name">>, <<"ping_net_resp">>}], Prop),
            Headers    = monitor_api:prepare_amqp_prop([Resp, Defaults]),
            {ok, JSON} = monitor_api:ping_net_resp(Headers),
            send_resp(JSON, RespQ, AHost);
        _ ->
            format_log(error, "MONITOR_AGENT_NETWORK.ping(~p): Failed to validate ping_net_req~n", [self()])
    end;

process_req(_MsgType, _Prop, _State) ->
    format_log(error, "MONITOR_AGENT_NETWORK(~p): Unhandled Msg ~p~nJSON: ~p~n", [self(), _MsgType, _Prop]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Places the given JSON into an AMQP payload on the provided hosts
%% targeted exchange for delievery to RespQ.
%%
%% @spec(send_resp/3 :: (JSON :: iolist(), RespQ :: binary(), 
%%      AHost :: string()) -> no_return()).
%% @end
%%--------------------------------------------------------------------
send_resp(JSON, RespQ, AHost) ->
    amqp_util:targeted_publish(AHost, RespQ, JSON, <<"application/json">>).
