%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Trunk-Store responder waits for Auth and Route requests on the broadcast
%%% Exchange, and delievers the requests to the corresponding handler.
%%% TS responder also receives responses from the handlers and returns them
%%% to the requester.
%%% Each request received by TS_RESPONDER should be put into a new spawn()
%%% to avoid blocking on each request.
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_responder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-define(SERVER, ?MODULE). 

-record(state, {channel, ticket, broad_q, tar_q}).

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
    {ok, Channel, Ticket, BroadQ, TarQ} = start_amqp(),
    {ok, #state{channel=Channel, ticket=Ticket, broad_q = BroadQ, tar_q = TarQ}}.

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
    format_log(error, "TS_RESPONDER(~p): Received EXIT(~p) from ~p, stopping...~n", [self(), Reason, _Pid]),
    {stop, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};
%% catch all so we dont loose state
handle_info(_Unhandled, State) ->
    format_log(info, "TS_RESPONDER(~p): unknown info request: ~p~n", [self(), _Unhandled]),
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
terminate(_Reason, #state{channel=Channel, ticket=Ticket, broad_q=BQ, tar_q=TQ}) ->
    amqp_channel:cast(Channel, amqp_util:queue_delete(Ticket, BQ)),
    amqp_channel:cast(Channel, amqp_util:queue_delete(Ticket, TQ)),
    format_log(error, "TS_RESPONDER(~p): Going down(~p)...~n", [self(), _Reason]),
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
    format_log(info, "TS_RESPONDER(~p): Code Change called~n", [self()]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_req(ContentType, Payload, State) ->
    case ContentType of
	<<"application/json">> ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "TS_RESPONDER(~p): Recv CT: ~p~nPayload: ~p~n", [self(), ContentType, Prop]),
	    process_req(get_msg_type(Prop), Prop, State);
	_ ->
	    format_log(info, "TS_RESPONDER(~p): recieved unknown msg type: ~p~n", [self(), ContentType])
    end.

get_msg_type(Prop) ->
    { get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop) }.

process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    case whistle_api:auth_req_v(Prop) andalso ts_auth:handle_req(Prop) of
	false ->
	    format_log(error, "TS_RESPONDER.auth(~p): Failed to validate auth_req~n", [self()]);
	{ok, JSON} ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    send_resp(JSON, RespQ, State);
	{error, _Msg} ->
	    format_log(error, "TS_RESPONDER.auth(~p) ERROR: ~p~n", [self(), _Msg])
    end;
process_req({<<"dialplan">>,<<"REQUEST_PARAMS">>}, Prop, #state{tar_q=TQ}=State) ->
    case whistle_api:route_req_v(Prop) andalso ts_route:handle_req(Prop, TQ) of
	false ->
	    format_log(error, "TS_RESPONDER.route(~p): Failed to validate route_req~n", [self()]);
	{ok, JSON} ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    send_resp(JSON, RespQ, State);
	{error, _Msg} ->
	    format_log(error, "TS_RESPONDER.route(~p) ERROR: ~p~n", [self(), _Msg])
    end;
process_req({<<"dialplan">>,<<"Call-Control">>}, Prop, State) ->
    3;
process_req(_MsgType, _Prop, _State) ->
    io:format("Unhandled Msg ~p~nJSON: ~p~n", [_MsgType, _Prop]).

send_resp(JSON, RespQ, #state{channel=Channel, ticket=Ticket}) ->
    {BP, AmqpMsg} = amqp_util:targeted_publish(Ticket, RespQ, JSON, <<"application/json">>),
    format_log(info, "TS_RESPONDER(~p): Sending to ~p~n", [self(), RespQ]),
    amqp_channel:call(Channel, BP, AmqpMsg).

start_amqp() ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    BroadName = ["ts_responder.", net_adm:localhost()],
    TarName = ["ts_responder.callctl.", net_adm:localhost()],

    amqp_channel:cast(Channel, amqp_util:broadcast_exchange(Ticket)),
    amqp_channel:cast(Channel, amqp_util:targeted_exchange(Ticket)),

    #'queue.declare_ok'{queue=BroadQueue} = amqp_channel:call(Channel, amqp_util:new_broadcast_queue(Ticket, BroadName)),
    #'queue.declare_ok'{queue=TarQueue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, TarName)),

    %% Bind the queue to an exchange
    amqp_channel:cast(Channel, amqp_util:bind_q_to_broadcast(Ticket, BroadQueue)),
    format_log(info, "TS_RESPONDER(~p): Bound ~p~n", [self(), BroadQueue]),
    amqp_channel:cast(Channel, amqp_util:bind_q_to_targeted(Ticket, TarQueue, TarQueue)),

    %% Register a consumer to listen to the queue
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, BroadQueue), self()),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, TarQueue), self()),

    format_log(info, "TS_RESPONDER(~p): Consuming on B(~p) and T(~p)~n", [self(), BroadQueue, TarQueue]),
    {ok, Channel, Ticket, BroadQueue, TarQueue}.
